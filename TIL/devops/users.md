## Deploy user
# Creating a user special for deployments

$ sudo useradd --create-home -s /bin/bash deploy
$ sudo adduser deploy sudo
$ sudo passwd deploy

and set his new password

## Create a deployers group
$ sudo groupadd deployers
## Add user to deployers group
$ sudo usermod -a -G deployers deploy
# or
$ sudo adduser deploy deployers
# list groups in which user is enrolled
$ groups deploy
# make /var/www editable by deployers
$ sudo mkdir /var/www
$ sudo chgrp deployers /var/www
$ sudo chmod g+w /var/www

## Passwordless SSH-ing

$ cat ~/.ssh/id_rsa.pub | pbcopy
$ ssh deploy@${HOST}
$ mkdir ~/.ssh
$ exit
$ scp ~/.ssh/id_rsa.pub deploy@${HOST}:~/.ssh/authorized_keys

## Sometimes "flightplan" for example need an SSH Agent to paswordlessly deploy.
# This can fail saying "All configured authentication methods failed". This is
because bla bla bla agent not saved ssh rsa key. Try:
$ ssh-add -l
# If no output:
$ ssh-add ~/.ssh_rsa (path to private key)
# Sometimes this will not persist and will lose state between boots. Keychain
# can store those keys for you:
$ ssh-add -K /path/to/private/key

## Disabling root access (generally a good idea on any server)
$ sudo vim /etc/ssh/sshd_config
=> PermitRootLogin yes
# and restart the ssh service
$ sudo service ssh restart

# BAM: no more root access

