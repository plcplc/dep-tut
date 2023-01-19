#!/usr/bin/env bash
# nix build -f ./nix/shell.nix --no-link
# nix-shell -p "import ./nix/shell.nix" --run ./workflows/tmux-dev.sh
nix-shell "./nix/shell.nix" --run ./workflows/tmux-dev.sh && tmux attach
