## NGINX
# This will use nginx as a proxy to some local internal app
# (like a node app)

$ sudo vim /etc/nginx/sites-available/default

server {
        listen 80;

        server_name foo.com;

        location / {
                proxy_pass http://127.0.0.1:3000;
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection 'upgrade';
                proxy_set_header Host $host;
                proxy_cache_bypass $http_upgrade;
        }
}

## Caching static stuff:

  location ~* \.(css|js|gif|jpe?g|png)$ {
    expires 168h;
  }

# => ~ means start case-sensitive regex. ~* means start case-incensitive regex


# Or you write it as a proxy to an upstream like this:
upstream nodejs {
    server 127.0.0.1:3000;
    keepalive 64;
}

server {
  listen 80;
  server_name 127.0.0.1;
  access_log /var/log/nginx/test.log;
  location / {
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header Host  $http_host;
    proxy_set_header X-Nginx-Proxy true;
    proxy_set_header Connection "";
    proxy_pass      http://nodejs;
  }
}
