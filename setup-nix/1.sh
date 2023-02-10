#!/usr/bin/env bash
set -eu

echo "Manually running nix/install script, fixed to nix 2.0 currently"
# curl https://nixos.org/nix/install | sh
cd /tmp
curl -o install-2.0 https://releases.nixos.org/nix/nix-2.0/install
chmod +x install-2.0
sh ./install-2.0

# this is only needed for server acting as targets for copy closure, but harmless to apply everywhere
echo "Repeat sourcing nix into .bashrc so that nix is available for copy closure"
# below was modeled after nix install script
p=$HOME/.nix-profile/etc/profile.d/nix.sh
echo "if [ -e $p ]; then . $p; fi # added by Nix installer" >> "$HOME/.bashrc"
