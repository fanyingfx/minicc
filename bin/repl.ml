open Minicc
let str = {|
int main(void){
{
int a = 1;
int b = a;
}

}
|};;
let tokens = Lexer.lex str;;
let ast = Parser.parse tokens;
