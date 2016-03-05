# Install vagrant and virtualbox
$ cask install vagrant
$ cask install virtualbox

# find a box on vagrantcloud.com and
$ vagrant box add hashicorp/precise32

# create a folder for project and init vagrant
$ vagrant init hashicorp/precise32

# run it
$ vagrant up

# ssh into it
$ vagrant ssh



## Make it into a real machine!
open vagrant file and uncomment:
config.vm.network "private_network", ip: "xx.xx.xx.xx" <= choose whatever ip you want!

# make a fake domain for that ip
$ sudo vim /etc/hosts
xx.xx.xx.xx mywebsite.com

# reload the box
$ vagrant reload

# install nginx
$ vagrant ssh
$ sudo apt-get install nginx ## might need a sudo apt-get update

# start nginx server
$ sudo service nginx start

# Go to mywebsite.com and you should see: "Welcome to Nginx!"
