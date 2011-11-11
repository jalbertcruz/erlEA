
Dir.chdir('Utilities/License_aplicator/')
puts Dir.pwd
puts %x{ant.bat run}

Dir.chdir('../Update_erl_from_clj/')
puts Dir.pwd
puts %x{ant.bat runMasterSlaveModel}
puts %x{ant.bat runIslandModel}