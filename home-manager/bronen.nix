{ pkgs, lib, ... }: {
  nixpkgs.config.allowUnfree = true;

  xsession.windowManager.i3 = {
    enable = true;
    config = let
      mod = "Mod4";
    in {
      modifier = mod;
      fonts = { names = ["MonaspiceKr Nerd Font Mono"]; size = "10.0"; };
      window = {
        # for_window [class=\"^.*\"] border pixel 0
        commands = [
          { criteria = { class = "^.*"; };
            command = "border pixel 0"; }
        ];
        hideEdgeBorders = "both";
      };
      bars = [
        { fonts = { names = ["MonaspiceKr Nerd Font Mono"]; size = "12.0"; };
          statusCommand = "${pkgs.i3status}/bin/i3status"; }
      ];
      keybindings = lib.mkOptionDefault {
        "XF86AudioRaiseVolume" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status";
        "XF86AudioLowerVolume" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status";
        "XF86AudioMute" = "exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status";
        "XF86AudioMicMute" = "exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status";
      };
    };
  };

  programs.i3status = {
    enable = true;
    general = { colors = true; interval = 5; };
    modules = {
      ipv6 = { enable = false; };

      "disk /" = {
        position = 4;
        settings = { format = "Avail: %avail"; };
      };

      memory = {
        position = 6;
        settings = {
          format = "%used of %available";
          threshold_degraded = "1G";
          format_degraded = "MEMORY < %available";
        };
      };
    };
  };

  programs.kitty = lib.mkForce {
    enable = true;
    font = {
      name = "MonaspiceKr Nerd Font Mono";
      size = 14;
    };
    settings = {
      confirm_os_window_close = 0;
      dynamic_background_opacity = false;
      enable_audio_bell = false;
      mouse_hide_wait = "-1.0";
      window_padding_width = 10;
      background_blur = 0;
      background_image = "~/Pictures/kitty-wallpaper.jpg";
      background_image_layout = "cscaled";
      background_tint = "0.99";
      symbol_map = let
        mappings = [
          "U+23FB-U+23FE"
          "U+2B58"
          "U+E200-U+E2A9"
          "U+E0A0-U+E0A3"
          "U+E0B0-U+E0BF"
          "U+E0C0-U+E0C8"
          "U+E0CC-U+E0CF"
          "U+E0D0-U+E0D2"
          "U+E0D4"
          "U+E700-U+E7C5"
          "U+F000-U+F2E0"
          "U+2665"
          "U+26A1"
          "U+F400-U+F4A8"
          "U+F67C"
          "U+E000-U+E00A"
          "U+F300-U+F313"
          "U+E5FA-U+E62B"
        ];
      in
        (builtins.concatStringsSep "," mappings) + " Symbols Nerd Font";
    };
  };

  programs.bash = {
    enable = true;
    shellAliases = {
      ll = "ls -l --color";
      la = "ls -las --color";
    };
    initExtra = ''
          . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
        '';
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.emacs.enable = true;

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    vimAlias = true;
    viAlias = true;
  };

  programs.git = {
    enable = true;
    userName = "BRonen";
    userEmail = "brennopereira6@gmail.com";
  };

  home.packages = with pkgs; [
    telegram-desktop discord slack
    xclip fortune ponysay flameshot
    code-cursor
    bitwarden
    spotify
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
    TERMINAL = "kitty";
    TERM = "kitty";
  };

  home = {
    username = "bronen";
    homeDirectory = "/home/bronen";
    file."wasdwasd" = {
      target = ".config/doom";
      source = ../doom;
      recursive = true;
    };
    file."wasdwasdw" = {
      target = ".config/nvim";
      source = ../nvim;
      recursive = true;
    };
  };

  home.stateVersion = "24.11";
}
