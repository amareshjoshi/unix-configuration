#
# copy config files into home directory, deleting
# any old files and folders first


$BASE = "C:\Users\joshia"
$REPO = "${BASE}\celta-vm-home-config"
$EMACS="${BASE}\.emacs.d"

#
# delete originals

Remove-Item "${BASE}\.bash_logout"
Remove-Item "${BASE}\.bash_profile"
Remove-Item "${BASE}\.bashrc"
Remove-Item "${BASE}\.tmux.conf"
Remove-Item -Recurse "${BASE}\.bash.d"

Remove-Item "${EMACS}\init.el"
Remove-Item -Recurse "${EMACS}\lisp"
Remove-Item -Recurse "${EMACS}\lisp-old"
Remove-Item -Recurse "${EMACS}\themes"

#
# now copy files from repo

Copy-Item "${REPO}\.bash_logout" -Destination "${BASE}"
Copy-Item "${REPO}\.bash_profile" -Destination "${BASE}"
Copy-Item "${REPO}\.bashrc" -Destination "${BASE}"
Copy-Item "${REPO}\.tmux.conf" -Destination "${BASE}"
Copy-Item "${REPO}\.bash.d" -Destination "${BASE}" -Recurse  -Force

Copy-Item "${REPO}\.emacs.d" -Destination "${BASE}" -Recurse -Force





