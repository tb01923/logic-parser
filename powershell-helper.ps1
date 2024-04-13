$rescript_test = 'C:\Program Files\nodejs\node_modules\rescript-test\bin\'
$PATH = [Environment]::GetEnvironmentVariable("PATH")
[Environment]::SetEnvironmentVariable("PATH", "$PATH;$rescript_test")