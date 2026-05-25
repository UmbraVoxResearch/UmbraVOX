# Builder VM tier: network + nix daemon.
# Can build nix derivations and fetch from cache.nixos.org.
# Used by: VM image builder, Signal Server build.
#
# Tier hierarchy:
#   base → network → builder → dev
{ config, lib, modulesPath, pkgs, ... }:

{
  imports = [ ./network.nix ];

  # Nix daemon for in-VM builds
  # vm-base.nix sets nix.enable = false; override here.
  nix.enable = lib.mkForce true;
  nix.settings = {
    experimental-features = [ "nix-command" ];
    sandbox = false;          # nested sandboxing is unreliable in VMs
    trusted-users = [ "root" ];
  };
  nix.nrBuildUsers = 4;

  environment.systemPackages = with pkgs; [
    nix
    git           # needed for nix fetchers (fetchFromGitHub, etc.)
  ];
}
