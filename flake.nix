{
  description = "GNU Emacs 31 with Commercial Gnus and GTK worker-wait fixes";
  outputs = { self }: {
    overlays.default = final: prev: {
      emacs31-pgtk-commercial-gnus = prev.emacs31-pgtk.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          ./patches/gnu-emacs31-glib-worker-wait.patch
        ];
        postPatch = (old.postPatch or "") + ''
          # Replace the complete built-in tree before autoload generation and
          # byte/native compilation.  No competing Gnus copy is installed.
          rm -rf lisp/gnus
          cp -R ${./lisp/gnus} lisp/gnus
          chmod -R u+w lisp/gnus
        '';
        passthru = (old.passthru or { }) // {
          commercialGnusRevision = "bf4184f985d0674a7c03c12eb33b66ac833d9e29";
          commercialGnusVersion = "0.3.0";
        };
      });
    };
  };
}
