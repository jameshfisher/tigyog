Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/trusty64"
  config.vm.provision "puppet"
  config.vm.provider "virtualbox" do |v|
    v.memory = 1024
  end
end