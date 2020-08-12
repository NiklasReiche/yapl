package yapl

import yapl.language.{Global, Import}

import scala.io.Source
import scala.util.Using

object Preprocessor {
    def importModules(path: String, imports: List[Import]): List[Global] =
    imports.foldLeft(Nil: List[Global])((result, imp) => {
        val program = Using(Source.fromFile(path + imp.file + ".yapl")) {
            _.mkString
        }

        if (program.isFailure) {
            println(s"Cannot find module ${imp.file}")
            return result
        }

        val tokens = Lexer.lex(program.get, imp.file)
        val (_, globals, imports) = Parser.parse(tokens)
        result concat globals concat importModules(path, imports)
    })
}
