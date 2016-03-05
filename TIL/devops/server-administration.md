## Setup locale
$ sudo dpkg-reconfigure tzdata


---


## Rocking a firewall (linux comes with ufw)
# https://www.digitalocean.com/community/tutorials/additional-recommended-steps-for-new-ubuntu-14-04-servers
# Allow ssh
$ sudo ufw allow ssh
# If ssh is not on default port
$ sudo ufw allow xxx/tcp
# If rocking a server like nginx, allow port 80
$ sudo ufw allow 80/tcp
# mail would be 25/tcp, SSL/TLS would be 443/tcp
# Show a list of all added:
$ sudo ufw show added
# Launch the firewall
$ sudo ufw enable


---


## activating SWAP file
# fallocate creates a pre-allocated file of certain size
$ sudo fallocate -l 4G /swapfile
# restrict other processes of writing to it
$ sudo chmod 600 /swapfile
# Tell system to format it for swap
$ sudo mkswap /swapfile
# Tell system to use the swapfile
$ sudo swapon /swapfile
# Now the system will use it for this session. Make sure it will use it at boot
$ sudo sh -c 'echo "/swapfile none swap sw 0 0" >> /etc/fstab'


---


## Installing a minecraft server :)
$ sudo apt-get update
# you can see possibilities when running java -version
$ sudo apt-get install default-jre
$ curl -LO https://s3.amazonaws.com/Minecraft.Download/versions/1.8.8/minecraft_server.1.8.8.jar
# might need 512M if digitalocean bassic
$ java -Xmx1024M -Xms1024M -jar ./minecraft_server.1.8.8.jar nogui


---


## notes
Sometimes crontab is off by a timezone. If you've update your locale recently,
don't forget to:
$ sudo service cron restart
