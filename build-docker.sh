nix-build docker.nix -A chainwebBaseImage -o chainweb-base &&
docker load < chainweb-base

nix-build docker.nix -A chainwebNodeImage -o chainweb-node &&
docker load < chainweb-node

nix-build docker.nix -A chainwebMinerImage -o chainweb-miner &&
docker load < chainweb-miner
