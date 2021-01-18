FROM hseeberger/scala-sbt:8u222_1.3.5_2.13.1
WORKDIR /XCOM
ADD . /XCOM
CMD sbt test
CMD sbt run

#docker build -t  xcom:v1 .
#docker run -ti  xcom:v1