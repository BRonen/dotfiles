{ config, pkgs, lib, ... }: {
  nixpkgs.config.allowUnfree = true;

  nix = {
    package = pkgs.nix;
    settings.experimental-features = [ "nix-command" "flakes" ];
  };

  programs.bash = {
    enable = true;
    initExtra = ''
      . "$HOME/.nix-profile/etc/profile.d/nix.sh";
      export PATH="$HOME/development/flutter/bin:$PATH";
      export PS1="\[\e[31m\][\[\e[m\]\[\e[38;5;172m\]\u\[\e[m\]@\[\e[38;5;153m\]\h\[\e[m\] \[\e[38;5;214m\]\W\[\e[m\]\[\e[31m\]]\[\e[m\]\\$ ";
      export PATH="$PATH":"$HOME/.pub-cache/bin";
      export PATH="$PATH":"$HOME/android-studio/bin";
      export PATH="$PATH":"$HOME/.cargo/bin";
      export PATH="$PATH":"$HOME/Android/Sdk/platform-tools";
      export PATH="$PATH":"$HOME/github/dotfiles/doomemacs/bin"
      export EDITOR="nvim";
      export TERMINAL="xterm-256color";
      export TERM="xterm-256color";
      export DOOMDIR="$HOME/.config/doom";
      export EMACSDIR="$HOME/.config/emacs";
    '';
    shellAliases = {
      ll = "ls --color=auto -l";
      la = "ls --color=auto -la";
    };
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
    settings.user = {
      name = "BRonen";
      email = "brennopereira6@gmail.com";
    };
  };

  home.packages = with pkgs; [
    discord
    slack
    spotify
    neofetch
    bitwarden-desktop
    wakatime-cli
    nerd-fonts.symbols-only
    nerd-fonts.monaspace
    mermaid-cli
    ripgrep
    xclip
    jet
    jq
  ];

  home = {
    stateVersion = "25.11";

    username = "brenno.rodrigues";
    homeDirectory = "/home/brenno.rodrigues";

    file."nvim" = {
      target = ".config/nvim";
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/github/dotfiles/nvim";
    };

    file."doom" = {
      target = ".config/doom";
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/github/dotfiles/doom";
    };

    file."emacs" = {
      target = ".config/emacs";
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/github/dotfiles/doomemacs";
    };
  };
}
