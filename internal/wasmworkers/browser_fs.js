// Browser syscall/js hosts share the Emscripten runtime thread's FS, including
// C's file descriptors and cwd. Never create a separate MEMFS on each worker.
addToLibrary({
  $llgoBrowserFS__deps: ['llgo_browser_fs_call', 'llgo_browser_fs_result', '$stringToNewUTF8', '$UTF8ToString', 'malloc', 'free', '$FS'],
  $llgoBrowserFS__postset: 'llgoBrowserFS.install();',
  $llgoBrowserFS: {
    invoke(target, name, args) {
      const request = stringToNewUTF8(JSON.stringify({ target, name, args }, (key, value) =>
        ArrayBuffer.isView(value) ? { bytes: Array.from(value) } : value));
      let response = 0;
      try {
        const size = _llgo_browser_fs_call(request);
        response = _malloc(size);
        if (!response) throw Object.assign(new Error('filesystem response allocation failed'), { code: 'ENOMEM' });
        _llgo_browser_fs_result(request, response);
        const reply = JSON.parse(UTF8ToString(response));
        if (reply.error) throw Object.assign(new Error(reply.error.message), reply.error);
        if (name === 'read') args[1].set(reply.bytes);
        const value = reply.value;
        if (name === 'stat' || name === 'lstat' || name === 'fstat') {
          value.isDirectory = () => (value.mode & 16384) !== 0;
        }
        return value;
      } finally {
        _llgo_browser_fs_result(request, 0);
        _free(request);
        if (response) _free(response);
      }
    },
    install() {
      if (ENVIRONMENT_IS_NODE) {
#if PTHREADS
        // Node omits process.chdir on workers. The cwd belongs to the process.
        if (ENVIRONMENT_IS_PTHREAD) {
          globalThis.process.chdir = path => llgoBrowserFS.invoke('process', 'chdir', [path]);
          globalThis.process.cwd = () => llgoBrowserFS.invoke('process', 'cwd', []);
        }
#endif
        return;
      }
      globalThis.llgoAttachWasmFS(Module);
#if PTHREADS
      if (!ENVIRONMENT_IS_PTHREAD) return;
      for (const target of ['fs', 'process', 'path']) {
        const host = globalThis[target];
        for (const name of Object.keys(host)) {
          if (typeof host[name] !== 'function') continue;
          host[name] = (...args) => {
            const callback = typeof args[args.length - 1] === 'function' ? args.pop() : null;
            let value, error;
            try { value = llgoBrowserFS.invoke(target, name, args); }
            catch (e) { error = e; }
            // Callback exceptions belong to Go. Do not call a callback twice.
            if (callback) return callback(error || null, value);
            if (error) throw error;
            return value;
          };
        }
      }
#endif
    },
  },

  // Allocate response storage on the requesting worker. Allocating on the
  // browser main thread can contend on the shared allocator, where Atomics.wait
  // is forbidden. Keep only JS strings here between the two proxy calls.
  $llgoBrowserFSResponses: {},
  llgo_browser_fs_result__deps: ['$llgoBrowserFSResponses', '$stringToUTF8', '$lengthBytesUTF8'],
  llgo_browser_fs_result__proxy: 'sync',
  llgo_browser_fs_result__sig: 'vdd',
  llgo_browser_fs_result(request, response) {
    const text = llgoBrowserFSResponses[request];
    if (response) stringToUTF8(text, response, lengthBytesUTF8(text) + 1);
    delete llgoBrowserFSResponses[request];
  },
  llgo_browser_fs_call__deps: ['$llgoBrowserFSResponses', '$UTF8ToString', '$lengthBytesUTF8'],
  llgo_browser_fs_call__proxy: 'sync',
  // Called only from JS: numeric linear-memory offsets work for both widths.
  llgo_browser_fs_call__sig: 'dd',
  llgo_browser_fs_call(request) {
    let reply;
    try {
      const { target, name, args } = JSON.parse(UTF8ToString(request), (key, value) =>
        value && Array.isArray(value.bytes) ? new Uint8Array(value.bytes) : value);
      const host = globalThis[target];
      let value;
      if (target === 'fs' && !name.endsWith('Sync')) {
        let called = false;
        let error;
        host[name](...args, (err, result) => { called = true; error = err; value = result; });
        if (!called) throw new Error('Emscripten FS host did not complete synchronously');
        if (error) throw error;
      } else {
        value = host[name](...args);
      }
      reply = { value };
      if (target === 'fs' && name === 'read') reply.bytes = Array.from(args[1]);
    } catch (e) {
      reply = { error: { message: e.message, code: e.code || 'EIO' } };
    }
    const text = JSON.stringify(reply);
    llgoBrowserFSResponses[request] = text;
    return lengthBytesUTF8(text) + 1;
  },
});
