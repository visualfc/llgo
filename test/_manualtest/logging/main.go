package main

import (
	"log"
	"log/slog"
	"os"
)

func main() {
	log.SetFlags(log.Lshortfile)
	log.Println("via log package func")

	logger := log.New(os.Stderr, "", log.Lshortfile)
	logger.Printf("via *Logger method")

	sl := slog.New(slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{AddSource: true}))
	sl.Info("via slog text handler")

	slog.SetDefault(slog.New(slog.NewJSONHandler(os.Stdout, &slog.HandlerOptions{AddSource: true})))
	slog.Warn("via slog json handler")
}
