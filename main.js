const { parse: parse1_0 } = require("./parser/1_0/parser")
const { parse: parse2_0 } = require("./parser/2_0/parser")
const readline = require('readline');
const printVisitors = require("./commands/printVisitors")

const parse = parse2_0

const INPUT_COMMAND = "INPUT_COMMAND"
const NEXT_COMMAND = "NEXT_COMMAND"
const PRINT_COMMAND = "PRINT_COMMAND"

const optionsOptions = {
    INPUT_COMMAND: [],
    NEXT_COMMAND: ["input", "print"],
    PRINT_COMMAND: ["prior menu", "ast", "implicit statement", "explicit statement", "debruinj"]
}

const prompt = {
    INPUT_COMMAND: "statement",
    PRINT_COMMAND: "print",
    NEXT_COMMAND: "ready"
}

function replDemo() {

    let expression = null ;
    let command = NEXT_COMMAND

    const setPrompt = rl => {
        const myPrompt = `${prompt[command]}(${optionsOptions[command].join("|")})>`
        rl.setPrompt(myPrompt)
    }


    return new Promise(function(resolve, reject) {
        let rl = readline.createInterface(process.stdin, process.stdout)
        setPrompt(rl)
        rl.prompt();
        rl.on('line', function(line) {
            if( command === INPUT_COMMAND ) {
                expression = parse( line )
                command = NEXT_COMMAND
                setPrompt(rl)
            }
            else if (command === NEXT_COMMAND && ( line === "exit" || line === "quit" || line == 'q' || line == 'bye' ) ) {
                rl.close()
                return
            }
            else if (command === NEXT_COMMAND && ( line === "input" || line === "i" || line === "expression" || line === "e" )) {
                command = INPUT_COMMAND
                setPrompt(rl)
            }
            else if (command === NEXT_COMMAND && ( line === "print" )) {
                command = PRINT_COMMAND
                setPrompt(rl)
            }
            else if ( command === PRINT_COMMAND && line === "ast") {
                console.log( expression.accept(printVisitors.ast) )
            }
            else if ( command === PRINT_COMMAND && line === "implicit statement") {
                console.log( expression.accept(printVisitors.implicitStatement) )
            }
            else if ( command === PRINT_COMMAND && line === "explicit statement") {
                console.log( expression.accept(printVisitors.explicitStatement) )
            }
            else if ( command === PRINT_COMMAND && line === "debruinj") {
                console.log( expression.accept(printVisitors.implicitDebruinj) )
            }
            else if ( command === PRINT_COMMAND && line === "prior menu") {
                command = NEXT_COMMAND
                setPrompt(rl)
            }
            rl.prompt()

        }).on('close',function(){
            console.log('bye')
            resolve(null)
        });
    })
}

replDemo()