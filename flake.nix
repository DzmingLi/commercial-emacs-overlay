{
  description = "Native selected-window borders for GNU Emacs 31 PGTK";

  outputs = _: {
    overlays.default = _final: prev: {
      emacs31-pgtk-window-border = prev.emacs31-pgtk.overrideAttrs (old: {
        pname = "emacs-pgtk-window-border";
        patches = (old.patches or [ ]) ++ [
          ./patches/gnu-emacs31-window-border.patch
        ];
        passthru = (old.passthru or { }) // {
          windowBorderModeVersion = "0.1.0";
        };
        meta = (old.meta or { }) // {
          description = "GNU Emacs 31 PGTK with native selected-window borders";
          homepage = "https://github.com/DzmingLi/window-border-mode";
        };
      });
    };
  };
}
