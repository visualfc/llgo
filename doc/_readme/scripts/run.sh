#!/bin/bash
DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$DIR" || exit 1

python_cmd=python3
if [[ "${OS:-}" == "Windows_NT" ]]; then
  python_cmd=python
fi
"$python_cmd" -m venv .venv
# shellcheck source=/dev/null
if [[ -f .venv/Scripts/activate ]]; then
  source .venv/Scripts/activate
else
  source .venv/bin/activate
fi
pip3 install numpy

PYTHONPATH=""
PYTHONPATH=$(python -c "import os, sys; print(os.pathsep.join(sys.path))")
export PYTHONPATH

for sub in ./*/; do
  if grep -q "func main()" "$DIR/$sub"/*.go 2>/dev/null; then
    echo "Running examples in $sub"
    cd "$DIR/$sub" || exit 1
    llgo run .
  fi
done
