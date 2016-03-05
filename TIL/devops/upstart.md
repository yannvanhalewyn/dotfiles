## App as a service
# You can have your app run on startup or respawn when crashing using linux services

$ sudo vim /etc/init/${appname}.conf

start on filesystem and started networking
respawn
chdir /home/deploy/node-app
env NODE_ENV=production
env PORT=3000
exec /usr/bin/node bin/www >> $HOME/logs/node.log 2>&1

# Don't forget to create the $HOME/logs dir!
# Test the config file:
$ init-checkconfig /etc/init/nodeapp.conf


# Now you can use sudo start ${appname}, or stop, or restart it
# This is for root access, so you should give the deploy use access to
# services

$ echo "deploy ALL=(root) NOPASSWD: /sbin/restart node-app" >> /etc/sudoers
