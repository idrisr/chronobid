{ pkgs, ... }:
let user = "hippoid";
in {
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;
  networking.hostName = "bitnomial";
  environment.systemPackages = with pkgs; [ tmux dfc vifm neovim sysz tree ];
  environment.variables.EDITOR = "nvim";
  environment.interactiveShellInit = ''
    set -o vi
  '';

  system.stateVersion = "24.05"; # update to match flake

  security.sudo.wheelNeedsPassword = false;
  nix.settings.trusted-users = [ "@wheel" ];
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINsnWJFXUmPQeaDEAmN7Dwyulu2WAiNTd1FesWJFfyi/ hippoid@framework"
    ];
  };

  users.users.chronobid = {
    isSystemUser = true;
    group = "chronobid";
  };

  environment.etc."chronobid/data/orders.csv".source =
    ./chronobid/data/orders.csv;

  environment.etc."chronobid/server/frontend/index.html" = {
    text = builtins.readFile ./chronobid/server/frontend/index.html;
    user = "chronobid";
    group = "chronobid";
    mode = "0644";
  };

  environment.etc."chronobid/server/frontend/orderbook.html" = {
    text = builtins.readFile ./chronobid/server/frontend/orderbook.html;
    user = "chronobid";
    group = "chronobid";
    mode = "0644";
  };

  environment.etc."chronobid/server/frontend/docs.html" = {
    text = builtins.readFile ./chronobid/server/frontend/docs.html;
    user = "chronobid";
    group = "chronobid";
    mode = "0644";
  };

  environment.etc."chronobid/API.md" = {
    text = builtins.readFile ./chronobid/API.md;
    user = "chronobid";
    group = "chronobid";
    mode = "0644";
  };

  users.groups.chronobid = { };

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;
    virtualHosts."nocoverletter.idrisraja.com" = {
      enableACME = true;
      forceSSL = true;

      locations."/" = {
        proxyPass = "http://localhost:8080";
        proxyWebsockets = true;
      };
    };
  };

  security.acme = {
    acceptTerms = true;
    certs."nocoverletter.idrisraja.com".email = "idris.raja@gmail.com";
  };

  systemd.services.chronobid = {
    description = "Chronobid Matching Engine";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.chronobid}/bin/server";
      WorkingDirectory = "/etc/chronobid";
      Restart = "always";
      User = "chronobid";
      Group = "chronobid";
    };
  };
}
