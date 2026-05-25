# Network VM tier: base + DHCP, DNS, curl.
# Has outbound network access but NO nix daemon.
# Used by: CI test runner, Signal runtime, runtime with port forwarding.
#
# Tier hierarchy:
#   base → network → builder → dev
{ config, lib, modulesPath, pkgs, ... }:

{
  imports = [ ./base.nix ];

  # Enable networking with DHCP
  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    curl
    cacert        # CA certificates for HTTPS
    jq            # JSON processing (useful for health checks)
  ];
}
