#!/bin/sh

#This script will issue the command to update ca certs in ubuntu
#To import a new cert. Retrieve the certificate and place it in
#/usr/share/ca-certificates
# create a directory (chmod 755)
# place file in said directory (chmod 644)
#then execute this command
sudo dpkg-reconfigure ca-certificates
