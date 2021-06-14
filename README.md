# Doom emacs config

## GccEmacs

Tutorial: https://gist.github.com/AllenDang/f019593e65572a8e0aefc96058a2d23e

Throubleshooting: https://www.gitmemory.com/issue/hlissner/doom-emacs/4590/785777442 

Create macos app workaround:
```
cp -R /nix/store/yz3q6cclh7faxkj9b3d1r0dbx3b9r1a0-emacsGccDarwin/Applications/Emacs.app ~/Applications/
rm -rf ~/Applications/Gccemacs.app/Contents/native-lisp/
sudo cp -R  /nix/store/yz3q6cclh7faxkj9b3d1r0dbx3b9r1a0-emacsGccDarwin/lib/emacs/28.0.50/native-lisp ~/Applications/Gccemacs.app/Contents/
```

### macos
Update:
```
nix-env -iA emacsGccDarwin -f https://github.com/twlz0ne/nix-gccemacs-darwin/archive/master.zip
```
