{
  description = "GNU Emacs 31 with Commercial Gnus and GTK worker-wait fixes";
  outputs = { self }: {
    overlays.default = import ./overlay.nix;
  };
}
