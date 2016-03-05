## SSL
# Make ssl directory for nginx and create the keys
$ sudo mkdir /etc/nginx/ssl
$ openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout /etc/nginx/ssl/nginx.key -out /etc/nginx/ssl/nginx.crt
# The -x509 tells us we want a self signed certificate instead of a signing request
# -nodes == no passphrase. This way nginx can read the files without user
# intervention, aka on startup and reboot

## Let's encrypt
## =============

## Getting certificates
# Clone the let's encrypt repo
$ sudo git clone https://github.com/letsencrypt/letsencrypt /opt/letsencrypt
# Verify port 80 is free
$ netstat -na | grep ':80.*LISTEN'
# run letsencrypt
$ cd /opt/letsencrypt
$ ./letsencrypt-auto certonly --standalone
# Follow prompts. You now have an SSL certificate!!
$ sudo ls /etc/letsencrypt/live/${hostname}

## Configuring nginx
# Remove listening to port 80
server {
  # Setup SSL
  listen 443 ssl;
  server_name example.com www.example.com;
  ssl_certificate /etc/letsencrypt/live/example.com/fullchain.pem;
  ssl_certificate_key /etc/letsencrypt/live/example.com/privkey.pem;

  # Only use most secure protocols and cyphers
  ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
  ssl_prefer_server_ciphers on;
  ssl_ciphers AES256+EECDH:AES256+EDH:!aNULL;
}

# in another server block, redirect to https
server {
  listen 80;
  server_name example.com;
  return 301 https://$host$request_uri;
}

## RENEWING letsencrypt certificates
# Via webroot you don't need to shut down nginx. Letsencrypt client will
# actually write some data to a file in the root of the served files and they will
# be fetched by the letsencrypt server
# FIRST: setup nginx to serve those files
# In the 'listen 80' block:
location '/.well-known/acme-challenge' {
  default_type "text/plain";
  root        /tmp/letsencrypt-auto;
}

location / {
  return 301 https://$host$request_uri;
}
# now renew the cert
$ cd /opt/letsencrypt
$ ./letsencrypt-auto certonly -a webroot --agree-tos --renew-by-default --webroot-path=/tmp/letsencrypt-auto -d example.com -d www.example.com
# Reload nginx
$ sudo service nginx reload

## Automating renewal
$ vim /usr/local/etc/le-renew-webroot.ini
<< EOF
# Use a 4096 bit RSA key instead of 2048
rsa-key-size = 4096
email = yann.vanhalewyn@gmail.com
domains = example.com, www.example.com
webroot-path = /tmp/letsencrypt-auto
EOF

# Now you can run the renewal using the config file:
$ cd opt/letsencrypt
$ ./letsencrypt-auto certonly -a webroot --renew-by-default --config /usr/local/etc/le-renew-webroot.ini

# Download a nifty script to renew the script if needed:
$ sudo curl -L -o /usr/local/sbin/le-renew-webroot https://gist.githubusercontent.com/thisismitch/e1b603165523df66d5cc/raw/fbffbf358e96110d5566f13677d9bd5f4f65794c/le-renew-webroot
$ sudo chmod +x /usr/local/sbin/le-renew-webroot

# Add a crontab (root level) for the renewal
$ sudo crontab -e
30 2 * * 1 /usr/sbin/le-renew-webroot >> /var/log/le-renewal.log

# Now every monday at 2:30 AM the cert will be renewed if within 30 days of expiration

