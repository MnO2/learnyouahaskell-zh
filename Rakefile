deploy_default = "push"

# This will be configured for you when you run config_deploy
deploy_branch  = "master"

## -- Misc Configs -- ##

public_dir      = "public"    # compiled site directory
source_dir      = "source"    # source file directory
deploy_dir      = "_deploy"   # deploy directory (for Github pages deployment)
server_port     = "5000"      # port for preview server eg. localhost:4000

desc "Generate site"
task :generate do
  raise "### You haven't set anything up yet. First run `rake install` to set up an Octopress theme." unless File.directory?(source_dir)
  puts "## Generating Site"
  mkdir "#{public_dir}/" unless Dir.exists?("#{public_dir}/")
  mkdir "#{public_dir}/zh-tw" unless Dir.exists?("#{public_dir}/zh-tw")
  mkdir "#{public_dir}/zh-cn" unless Dir.exists?("#{public_dir}/zh-cn")
  system "ruby parse.rb"
  cp_r "#{source_dir}/img", public_dir
  cp_r "#{source_dir}/css", public_dir
  cp_r "#{source_dir}/js", public_dir
  cp "#{source_dir}/CNAME", "#{public_dir}/CNAME"
  cp "#{source_dir}/img/favicon.png", "#{public_dir}/favicon.ico"
  cp "#{source_dir}/index.html", "#{public_dir}/index.html"
end

##############
# Deploying  #
##############

desc "Default deploy task"
task :deploy do
  Rake::Task[:generate].invoke()
  Rake::Task["#{deploy_default}"].execute
end

desc "copy dot files for deployment"
task :copydot, :source, :dest do |t, args|
  exclusions = [".", "..", ".DS_Store"]
  Dir["#{args.source}/**/.*"].each do |file|
    if !File.directory?(file) && !exclusions.include?(File.basename(file))
      cp(file, file.gsub(/#{args.source}/, "#{args.dest}"));
    end
  end
end

desc "deploy public directory to github pages"
multitask :push do
  puts "## Deploying branch to Github Pages "
  (Dir["#{deploy_dir}/*"]).each { |f| rm_rf(f) }
  Rake::Task[:copydot].invoke(public_dir, deploy_dir)
  puts "\n## copying #{public_dir} to #{deploy_dir}"
  cp_r "#{public_dir}/.", deploy_dir
  cd "#{deploy_dir}" do
    system "git add ."
    system "git add -u"
    puts "\n## Commiting: Site updated at #{Time.now.utc}"
    message = "Site updated at #{Time.now.utc}"
    system "git commit -m \"#{message}\""
    puts "\n## Pushing generated #{deploy_dir} website"
    system "git push origin #{deploy_branch} --force"
    puts "\n## Github Pages deploy complete"
  end
end

desc "preview the site in a web browser"
task :preview do
  raise "### You haven't set anything up yet. First run `rake install` to set up an Octopress theme." unless File.directory?(source_dir)
  puts "Starting Rack on port #{server_port}"
  rackupPid = Process.spawn("rackup --port #{server_port}")

  trap("INT") {
    [rackupPid].each { |pid| Process.kill(9, pid) rescue Errno::ESRCH }
    exit 0
  }

  [rackupPid].each { |pid| Process.wait(pid) }
end


desc "list tasks"
task :list do
  puts "Tasks: #{(Rake::Task.tasks - [Rake::Task[:list]]).join(', ')}"
  puts "(type rake -T for more detail)\n\n"
end
