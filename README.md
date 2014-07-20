# Erlang Tutorial - OSCON 2014

## Using Vagrant

### Step 1 - Install Vagrant

If you don't already have Vagrant installed on your system, download or install
a package based on the help here:

https://www.vagrantup.com/downloads.html

### Step 2 - Get the Erlang Box

Run this command:

    $ vagrant box add ubuntu-erlang https://dl.dropboxusercontent.com/s/37x8l9myg45ql2e/elixir.box

This will download a Vagrant box containing Erlang R17. It will take a while as
it downloads an entire OS image for use in Vagrant/Virtual Box.

### Step 3 - Setup a Vagrant Project

Create a directory to contain the Vagrant project and initialize a Vagrant
file:

    $ mkdir erlang-tutorial
	$ cd erlang-tutorial
	$ vagrant init

### Step 4 - Start the Vagrant Server

While in the Vagrant project directory, run:

    $ vagrant up

This will start a Virtual Box process using the Erlang box.

### Step 5 - Connect to the Tutorial Server

While in the Vagrant project directly with the server 'up', run:

    $ vagrant ssh

This will connect you to the running server.

The remaining steps will be executed as the 'vagrant' user in the locally
running server over ssh. To clarify that a command should be run from while
ssh'd to the server, the prompts below are `vagrant:$`

### Step 6 - Install git and Clone the Tutorial Project

From *inside the Vagrant server*, run:

    vagrant:$ sudo apt-get install -y git
	vagrant:$ git clone https://github.com/gar1t/oscon-2014-tutorial

### Step 7 - Install Tools of Your Choice

`vi` and `nano` are already installed on the Vagrant box. If you'd prefer
another editor (e.g. Emacs, Joe, etc.) install it as an Ubuntu package.

You may also find it useful to install a multiplexer like `tmux` or
`screen`. You can alternatively establish multiple connections to the Vagrant
server over ssh using `vagrant ssh`.

Ubuntu packages you might want to install:

Editors:

- emacs23-nox
- joe

Multiplexers:

- tmux
- screen

#### Emacs Configuration

If you don't already have an Emacs configuration handy, consider installing the
one from the tutorial project. This configuration in particular has a key
binding to speed up the compilation process from within Emacs.

    vagrant:$ cp ~/oscon-2014-tutorial/emacs/init.el ~/.emacs.d/

You can type C-x C-m to get a prompt to run a command within the current file's
directory. The default command is `make -k`. Pressing Enter will start the make
process.
