els-settings-test:
    emacs --batch -l ./typst-ts-embedding-lang-settings.el \
    -l ~/.emacs.d/.local/elpaca/repos/emacs-kotlin-ts-mode/kotlin-ts-mode.el \
    --eval "(typst-ts-embedding-lang-settings-test)"
