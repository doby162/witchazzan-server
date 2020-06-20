while (1) {
  cd ..\witchazzan-client
  git pull
  cd ..\witchazzan-server
  git pull
  ./lein.bat uberjar
  java -jar .\target\uberjar\witchazzan-server.jar
  mv -Force .\config\log D:\Dropbox\Witchazzan\latest-log\log.txt
}