
Dir.chdir('./MasterSlaveModel/src')
puts 'Compilando MasterSlaveModel'

Dir["*.erl"].each { |e|
 puts %x{"erlc" #{e}}
}

Dir.chdir('../../IslandModel/src')
puts 'Compilando IslandModel'

Dir["*.erl"].each { |e|
 puts %x{"erlc" #{e}}
}