while (1) {
  cd ..\witchazzan-client
  git pull
  cd ..\witchazzan-server
  git pull
  # Without the cmd /C the entire batch file exits when lein exits, because it is a batch file itself.
  cmd /C lein.bat trampoline run
  del config\save.edn
}