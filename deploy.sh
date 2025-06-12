#!/usr/bin/env bash
set -euo pipefail

SERVER=$(cat nixaws)
PROFILE="/nix/var/nix/profiles/system"

echo ">> Building system..."
OUT=$(nix build --no-link --print-out-paths .#nixosConfigurations.bitnomial.config.system.build.toplevel --impure)

echo ">> Copying closure to $SERVER..."
nix copy --to ssh://root@"$SERVER" "$OUT"

echo ">> Activating new system..."
ssh root@$SERVER "sudo nix-env --profile $PROFILE --set $OUT && sudo $PROFILE/bin/switch-to-configuration switch"

echo ">> Done."
