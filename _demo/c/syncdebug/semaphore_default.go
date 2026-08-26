//go:build !darwin

package main

func initWorkerSemaphore() bool {
	return workerSem.Init(0, 0) == 0
}

func postWorkerSemaphore() bool {
	return workerSem.Post() == 0
}

func waitWorkerSemaphore() bool {
	return workerSem.Wait() == 0
}

func destroyWorkerSemaphore() bool {
	return workerSem.Destroy() == 0
}
