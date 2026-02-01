{ config, pkgs, lib, ... }: {
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

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

  xdg.configFile."ghostty/config".text = ''
      theme = dark:catppuccin-latte,light:catppuccin-mocha
      command = /bin/zsh --login
      font-family = "MonaspiceKr Nerd Font Mono"
      window-decoration = false
      macos-option-as-alt = true
  '';

  home = {
    stateVersion = "26.05";

    username = "bronen";
    homeDirectory = "/Users/bronen";

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
