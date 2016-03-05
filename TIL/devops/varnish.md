## VARNISH
#
# INSTALLATION
$ sudo apt-get install apt-transport-https
$ curl https://repo.varnish-cache.org/GPG-key.txt | sudo apt-key add -
$ echo "deb https://repo.varnish-cache.org/ubuntu/ trusty varnish-4.1" >> /etc/apt/sources.list.d/varnish-cache.list
$ apt-get update
$ apt-get install varnish

## Now going to <ip>:6081 should connect to varnish. Varnish tries to proxy to
# port 8080, so you might get "Error 503 service unavailable"
$ sudo vim /etc/varnish/default.vcl
vcl 4.0;

backend default {
    .host = "127.0.0.1";
    .port = "80"; <= CHANGE THIS
}

# Now connect to <ip>:6081 again, and it should connect to nginx or whatever
# server is running

