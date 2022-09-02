const { getParser } = require("./parser/parser")
const parser = getParser()
const readline = require('readline');

function replDemo() {
    return new Promise(function(resolve, reject) {
        let rl = readline.createInterface(process.stdin, process.stdout)
        rl.setPrompt('ready> ')
        rl.prompt();
        rl.on('line', function(line) {
            if (line === "exit" || line === "quit" || line == 'q') {
                rl.close()
                return // bail here, so rl.prompt() isn't called again
            }

            const x = parser.parse(line)
            console.log(JSON.stringify(x, null, 4))

            rl.prompt()

        }).on('close',function(){
            console.log('bye')
            resolve(42) // this is the final result of the function
        });
    })
}

replDemo()