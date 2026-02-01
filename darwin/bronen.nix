{ self, pkgs, ... }: {
  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    
    brews = [
      "mas"
    ];

    casks = [
      "discord"
      "spotify"
      "signal"
      "google-chrome"
      "raycast"
      "emacs-app"
      "orbstack"
      "via"
      "ghostty"
    ];

    masApps = {
      "Telegram" = 747648890;
      "Slack" = 803453959;
      "Bitwarden" = 1352778147;
      "Whatsapp" = 310633997;
    };
  };

  environment.systemPackages = with pkgs; [
    git
    neovim
    fzf
    jq
    wakatime-cli
    jet
    neofetch
    direnv
    nix-direnv
    aerospace
    terminal-notifier
    oh-my-zsh
    zsh-powerlevel10k
    zsh-autosuggestions
    zsh-syntax-highlighting
  ];

  services.aerospace = {
    enable = true;
    
    settings = {
      config-version = 2;
      
      persistent-workspaces = ["1" "2" "3" "4" "5" "6" "7" "8" "9"];
      
      gaps = {
        inner.horizontal = 0;
        inner.vertical   = 0;
        outer.left       = 0;
        outer.bottom     = 0;
        outer.top        = 0;
        outer.right      = 0;
      };

      mode.main.binding = {
        "alt-d" = "layout floating tiling";
        "alt-slash" = "layout tiles horizontal vertical";
        "alt-comma" = "layout accordion horizontal vertical";
        "alt-h" = "focus left";
        "alt-j" = "focus down";
        "alt-k" = "focus up";
        "alt-l" = "focus right";
        "alt-shift-h" = "move left";
        "alt-shift-j" = "move down";
        "alt-shift-k" = "move up";
        "alt-shift-l" = "move right";
        "alt-minus" = "resize smart -50";
        "alt-equal" = "resize smart +50";
        "alt-1" = "workspace 1";
        "alt-2" = "workspace 2";
        "alt-3" = "workspace 3";
        "alt-4" = "workspace 4";
        "alt-5" = "workspace 5";
        "alt-6" = "workspace 6";
        "alt-7" = "workspace 7";
        "alt-8" = "workspace 8";
        "alt-9" = "workspace 9";
        "alt-shift-1" = "move-node-to-workspace 1";
        "alt-shift-2" = "move-node-to-workspace 2";
        "alt-shift-3" = "move-node-to-workspace 3";
        "alt-shift-4" = "move-node-to-workspace 4";
        "alt-shift-5" = "move-node-to-workspace 5";
        "alt-shift-6" = "move-node-to-workspace 6";
        "alt-shift-7" = "move-node-to-workspace 7";
        "alt-shift-8" = "move-node-to-workspace 8";
        "alt-shift-9" = "move-node-to-workspace 9";
        "alt-tab" = "workspace-back-and-forth";
        "alt-shift-tab" = "move-workspace-to-monitor --wrap-around next";
        "alt-shift-semicolon" = "mode service";
        "alt-enter" = "exec-and-forget open -a Ghostty";
      };

      mode.service.binding = {
        "esc" = ["reload-config" "mode main"];
      };

      on-window-detected = [
        {
          "check-further-callbacks" = true;
          "run" = "layout tiling";
        }
        {
          "if".app-id = "com.apple.systempreferences";
          "run" = "layout floating";
        }
      ];
    };
  };
  
  fonts.packages = [
    pkgs.nerd-fonts.symbols-only
    pkgs.nerd-fonts.monaspace
  ];

  nix.settings.experimental-features = "nix-command flakes";

  programs.zsh = {
    enable = true;

    interactiveShellInit = ''
      export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/
      eval "$(direnv hook zsh)"

      # ZSH_THEME=""

      plugins=(git node bun bgnotifier docker docker-compose emacs fzf gradle history macos npm themes xcode)

      source $ZSH/oh-my-zsh.sh
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      source ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
      source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    '';
  };

  users.users.bronen = {
    name = "bronen";
    home = "/Users/bronen";
    shell = pkgs.zsh;
  };

  system.configurationRevision = self.rev or self.dirtyRev or null;

  system.stateVersion = 5;

  system.primaryUser = "bronen";
  system.defaults = {
    dock.autohide = true;
    finder.AppleShowAllExtensions = true;
    NSGlobalDomain.AppleInterfaceStyle = "Dark";
  };
}
