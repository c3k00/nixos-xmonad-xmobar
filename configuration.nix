# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Minsk";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

	services.xserver.enable = true;
	services.xserver.displayManager.lightdm.enable = true;
	services.xserver.windowManager.xmonad.enable = true;
	services.xserver.windowManager.xmonad.enableContribAndExtras = true;
	services.xserver.windowManager.xmonad.extraPackages = haskellPackages: [
		haskellPackages.xmobar
];
#	services.xserver.displayManager.sessionCommands = ''
#		xrandr --output Virtual1 -- mode 1920x1080
#	'';
	
	services.xserver.xrandrHeads = [ "Virtual1" ];
	services.xserver.resolutions = [ { x = 1920; y = 1080; } ];

	programs.zsh.enable = true;
	programs.zsh.enableCompletion = true;
	programs.zsh.histSize = 5000;
	programs.zsh.enableBashCompletion = true;
	programs.zsh.autosuggestions.enable = true;
	programs.zsh.syntaxHighlighting.enable = true;
	programs.zsh.ohMyZsh.enable = true;
	programs.zsh.ohMyZsh.theme = "jonathan";
	programs.zsh.ohMyZsh.customPkgs = [
		pkgs.nix-zsh-completions
		pkgs.zsh-nix-shell
];
	users.defaultUserShell = pkgs.zsh;

	sound.enable = true;
#	hardware.pulseaudio.enable = true;
#	services.rtkit.enable = true;
	services.pipewire = {
		enable = true;
		alsa.enable = true;
		alsa.support32Bit = true;
		pulse.enable = true;
	};
	fonts.fonts = with pkgs; [
(nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" ]; })
];
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.c3k = {
    isNormalUser = true;
    description = "c3k";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #  wget
	git
	alacritty
	ranger
	neofetch
	vscode
	neovim
	xorg.xrandr
	python311
	brightnessctl
	opera
	rofi
	xfce.thunar
	feh
	haskellPackages.xmobar
	arandr
	gccgo13
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}