FROM hseeberger/scala-sbt
WORKDIR /XCOM
ADD . /XCOM
CMD sbt test