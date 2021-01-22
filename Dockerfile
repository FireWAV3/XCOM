FROM hseeberger/scala-sbt:8u222_1.3.5_2.13.1
RUN apt-get update && \
    apt-get install -y sbt libxrender1 libxtst6 libxi6
WORKDIR /XCOM
ADD . /XCOM
CMD sbt test
CMD sbt run

#docker build -t  xcom:v1 .
#docker run -ti --rm -e DISPLAY=172.18.13.225:0.0 xcom:v1
#docker run -ti  xcom:v1
