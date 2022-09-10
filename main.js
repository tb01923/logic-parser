const { parse: parse1_0 } = require("./parser/1_0/parser")
const { parse: parse2_0 } = require("./parser/2_0/parser")
const readline = require('readline');

const parse = parse2_0

const INPUT_COMMAND = "INPUT_COMMAND"
const NEXT_COMMAND = "NEXT_COMMAND"

function replDemo() {

    let expression = null ;
    let command = NEXT_COMMAND

    return new Promise(function(resolve, reject) {
        let rl = readline.createInterface(process.stdin, process.stdout)
        rl.setPrompt("ready(input|print)>")
        rl.prompt();
        rl.on('line', function(line) {
            if( command === INPUT_COMMAND ) {
                expression = parse( line )
                rl.setPrompt("ready(input|print)>")
                command = NEXT_COMMAND
            }
            else if (command === NEXT_COMMAND && ( line === "exit" || line === "quit" || line == 'q' || line == 'bye' ) ) {
                rl.close()
                return
            }
            else if (command === NEXT_COMMAND && ( line === "input" || line === "i" || line === "expression" || line === "e" )) {
                rl.setPrompt("expression>")
                command = INPUT_COMMAND
            }
            else if ( command === NEXT_COMMAND && line === "print") {
                console.log( expression.toString() )
            }


            rl.prompt()

        }).on('close',function(){
            console.log('bye')
            resolve(null)
        });
    })
}

replDemo()