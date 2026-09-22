# Module-mode Go profiles use import-qualified filenames. Keep the mode header
# and all non-test/ sources; only llgo test may report coverage of test/ fixtures.
NR == 1 || $0 !~ /^github[.]com\/xgo-dev\/llgo\/test\//
