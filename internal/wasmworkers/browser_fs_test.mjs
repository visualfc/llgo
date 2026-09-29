import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync(new URL('./browser_fs.js', import.meta.url), 'utf8')
  .replace(/^#.*$/gm, '');
for (const memory64 of [false, true]) {
  const heap = new Uint8Array(new SharedArrayBuffer(1 << 20));
  const allocated = new Set();
  let next = 64, largestRequest = 0;
  function malloc(size) {
    const pointer = next;
    next += size + 8;
    allocated.add(pointer);
    return memory64 ? BigInt(pointer) : pointer;
  }
  function free(pointer) { assert.ok(allocated.delete(Number(pointer))); }
  function stringToUTF8(value, pointer, size) {
    const bytes = new TextEncoder().encode(value);
    assert.ok(bytes.length < size);
    heap.set(bytes, Number(pointer));
    heap[Number(pointer) + bytes.length] = 0;
  }
  function UTF8ToString(pointer) {
    pointer = Number(pointer);
    return new TextDecoder().decode(heap.slice(pointer, heap.indexOf(0, pointer)));
  }
  function stringToNewUTF8(text) {
    largestRequest = Math.max(largestRequest, text.length);
    const pointer = malloc(new TextEncoder().encode(text).length + 1);
    stringToUTF8(text, pointer, Number(next) - Number(pointer));
    return pointer;
  }
  function context(pthread) {
    const ctx = vm.createContext({Uint8Array, ArrayBuffer, HEAPU8: heap, Module: {},
      ENVIRONMENT_IS_NODE: false, ENVIRONMENT_IS_PTHREAD: pthread,
      _malloc: malloc, _free: free, stringToUTF8, UTF8ToString, stringToNewUTF8,
      lengthBytesUTF8: value => new TextEncoder().encode(value).length,
      llgoAttachWasmFS() {},
      addToLibrary(lib) {
        for (const [name, value] of Object.entries(lib)) {
          if (!name.includes('__')) ctx[name.replace(/^\$/, '')] = value;
        }
      },
    });
    vm.runInContext(source, ctx);
    return ctx;
  }
  const main = context(false), worker = context(true);
  let writes = [];
  main.fs = {
    read(fd, buffer, offset, length, position, cb) {
      buffer.set([10, 20, 30].slice(0, length), offset);
      cb(null, Math.min(3, length));
    },
    write(fd, buffer, offset, length, position, cb) {
      writes.push(Array.from(buffer.subarray(offset, offset + length)));
      cb(null, length);
    },
    stat(path, cb) { cb(null, {mode: 16384}); },
  };
  worker.fs = {read() {}, write() {}, stat() {}};
  worker.process = {cwd() {}, chdir() {}};
  worker.path = {resolve() {}};
  worker._llgo_browser_fs_call = pointer => main.llgo_browser_fs_call(Number(pointer));
  worker._llgo_browser_fs_result = (request, response) => main.llgo_browser_fs_result(Number(request), Number(response));
  worker.llgoBrowserFS.install();
  const installedRead = worker.fs.read;
  worker.llgoBrowserFS.install();
  assert.equal(worker.fs.read, installedRead);
  const buffer = new Uint8Array(10000).fill(99);
  worker.fs.read(1, buffer, 7, 5, null, (err, n) => { assert.equal(err, null); assert.equal(n, 3); });
  assert.deepEqual(Array.from(buffer.slice(5, 12)), [99, 99, 10, 20, 30, 99, 99]);
  worker.fs.write(1, buffer, 7, 3, null, (err, n) => { assert.equal(err, null); assert.equal(n, 3); });
  assert.deepEqual(writes, [[10, 20, 30]]);
  assert.ok(largestRequest < 200, 'byte arrays must not travel through JSON');
  for (const [offset, length] of [[-1, 1], [10000, 1], [0, -1], [0.5, 1]]) {
    worker.fs.read(1, buffer, offset, length, null, err => assert.equal(err.code, 'EINVAL'));
  }
  main.fs.read = (fd, buffer, offset, length, position, cb) => cb(null, length + 1);
  worker.fs.read(1, buffer, 0, 3, null, err => assert.equal(err.code, 'EIO'));
  let callbacks = 0;
  assert.throws(() => worker.fs.read(1, buffer, 0, 3, null, () => { callbacks++; throw new Error('callback'); }), /callback/);
  assert.equal(callbacks, 1);
  worker.fs.stat('/', (err, st) => { assert.equal(err, null); assert.ok(st.isDirectory()); });
  for (const [target, name, args] of [
    ['process', 'exit', []], ['constructor', 'constructor', []], ['fs', '__proto__', []],
    ['fs', 'read', [1, {pointer: heap.length, length: 2}, 0, 2]],
  ]) {
    const request = stringToNewUTF8(JSON.stringify({target, name, args}));
    main.llgo_browser_fs_call(Number(request));
    const reply = JSON.parse(main.llgoBrowserFSResponses[Number(request)]);
    assert.ok(['EINVAL', 'ENOSYS'].includes(reply.error.code));
    main.llgo_browser_fs_result(Number(request), 0);
    free(request);
  }
  assert.equal(allocated.size, 0);
  assert.equal(Object.keys(main.llgoBrowserFSResponses).length, 0);
}
console.log('browser filesystem proxy contract passed');
