set -l commands completion convert debug deploy destroy diff get init list login output provider synth watch

complete -c cdktf -f

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a init -d 'Create a new cdktf project from a template.'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a get -d 'Generate CDK Constructs for Terraform providers and modules.'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a convert -d 'Converts a single file of HCL configuration to CDK for Terraform. Takes the file to be converted on stdin.'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a deploy -d 'Deploy the given stacks                                                                                                                        [aliases: apply]'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a destroy -d 'Destroy the given stacks'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a diff -d 'Perform a diff (terraform plan) for the given stack                                                                                             [aliases: plan]'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a list  -d 'List stacks in app.'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a login -d 'Retrieves an API token to connect to Terraform Cloud or Terraform Enterprise.'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a synth -d 'Synthesizes Terraform code for the given app in a directory.                                                                              [aliases: synthesize]'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a watch -d '[experimental] Watch for file changes and automatically trigger a deploy'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a output -d 'Prints the output of stacks                                                                                                                  [aliases: outputs]'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a debug   -d 'Get debug information about the current project and environment'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a provider -d 'A set of subcommands that facilitates provider management'

complete -c cdktf -n "not __fish_seen_subcommand_from $commands" \
  -a completion -d 'generate completion script'

