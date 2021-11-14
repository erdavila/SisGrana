if [ "$REBUILD" == "true" ]; then
  (
    cd $(dirname $0)/..
    sbt assembly
  )
fi

exec java -cp $(dirname $0)/../target/scala-2.13/SisGrana-assembly-0.1.jar $MAIN_CLASS "$@"
