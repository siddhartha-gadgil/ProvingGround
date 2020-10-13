# sudo apt update
# sudo apt install curl openjdk-11-jdk tmux
export THREADS=16
export JAVA_OPTS="-Xms512m -Xmx56g"
export JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64"
curl http://math.iisc.ac.in/~gadgil/jvmcore.jar -o jvmcore.jar 
chmod +x jvmcore.jar
tmux
./jvmcore.jar
