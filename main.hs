module Main where
import System.IO

--DEFINIÇÃO DO TIPO DE DADOS LIVRO
type NomeAutor = String
type Titulo = String
data Livro = Livro {
    titulo :: Titulo,
    autor :: NomeAutor
} deriving (Show,Eq)
----------------------------------------------------------------
--lISTA DE lIVROS DISPONÍVEIS NA BIBLIOTECA
livrosDaBiblioteca :: [Livro]
livrosDaBiblioteca = [
    Livro "O sertao vai virar mar" "Moacyr Scliar",
    Livro "Agua viva" "Clarice Lispector" ,
    Livro "Terror na Festa" "Janaina Amado",
    Livro "O carrasco que era Santo" "Josue Montello",
    Livro "O horizonte mora em um dia cinza" "Tatielle Katluryn",
    Livro "Enigma na televisao" "Marcos Rey",
    Livro "Acucar Amargo" "Luiz Puntel",
    Livro "Orgulho e preconceito" "Jane Austen",
    Livro "Antes que ele mate" "Black Pierce",
    Livro "A proxima porta" "Black Pierce",
    Livro "O Agito de Pilar no Egito" "Flavia Lins e Silva",
    Livro "Os miseraveis" "Victor Hugo",
    Livro "Os 13 porques" "Jay Asher",
    Livro "Pollyanna" "Eleanor H Porter",
    Livro "Pollyanna moca" "Eleanor H Porter",
    Livro "O Ditador" "Sidney Sheldon",
    Livro "Cinderela e o baile dela" "Janaina Tokitaka",
    Livro "O jogo do camaleao" " Marcal Aquino",
    Livro "Bisa Bia, bisa Bel" "Ana Maria Machado",
    Livro "A formiguinha e a neve" "João de Barro (Braguinha)"
    ]
----------------------------------------------------------------
--LISTA DE LIVROS EMPRESTADOS
livrosEmprestados :: [Livro]
livrosEmprestados = []
----------------------------------------------------------------
--FUNÇÃO QUE RETORNA UM LIVRO ESPECÍFICO
buscaLivro ::  [Livro] -> Livro -> Bool
buscaLivro [] _ = False
buscaLivro (livro: resto) nome  =
  if livro == nome then True
  else buscaLivro resto nome 
----------------------------------------------------------------
--FUNÇÃO PARA REALIZAR UM EMPRÉSTIMO DE LIVRO
emprestarLivro :: Livro -> ([Livro],[Livro]) -> (Bool,[Livro],[Livro])
emprestarLivro livro (disponiveis,emprestados)
 | buscaLivro disponiveis livro = (True,filter (/= livro) disponiveis, livro:emprestados)
 | otherwise = (False,disponiveis, emprestados)
----------------------------------------------------------------
--FUNÇÃO PARA REALIZAR A DEVOLUÇÃO
devolverLivro :: Livro -> ([Livro],[Livro]) -> (Bool,[Livro],[Livro])
devolverLivro livro (disponiveis,emprestados) 
    | buscaLivro emprestados livro = (True, livro:disponiveis, filter (/= livro) emprestados)
    | otherwise = (False,disponiveis, emprestados)
----------------------------------------------------------------
--FUNÇÃO PARA RETORNAR UMA LISTA DE LIVROS
buscaLivros ::  [Livro] -> String -> [Livro]
buscaLivros [] _ = []
buscaLivros (livro: resto) nome
  | titulo livro == nome = livro : buscaLivros resto nome
  | autor livro == nome = livro : buscaLivros resto nome
  | otherwise = buscaLivros resto nome
-----------------------------------------------------------------
--FUNÇÃO PARA A EXIBIÇÃO DOS LIVROS
imprimeLivros :: [Livro] -> String
imprimeLivros [] = []
imprimeLivros (livro:resto) = "\nTitulo do Livro: "++titulo livro++"\nNome do autor: "++ autor livro++"\n"++imprimeLivros resto
-----------------------------------------------------------------
--MENU DA BIBLIOTECA
menu :: [Livro] -> [Livro] -> IO ()
menu livrosDaBiblioteca livrosEmprestados = do
    putStrLn "----SEJA BEM VINDO AO SISTEMA SILIB---\n"
    putStrLn "|  O QUE DESEJA FAZER?                |\n"
    putStrLn "|  1 - BUSCA DE LIVRO                 |\n"
    putStrLn "|  2 - EMPRESTIMO DE LIVRO            |\n"
    putStrLn "|  3 - DEVOLUCAO DE LIVRO             |\n"
    putStrLn "|  4 - VER LIVROS EMPRESTADOS         |\n"
    putStrLn "|  5 - ADICIONAR LIVRO                |\n"
    putStrLn "|  0 - SAIR                           |\n"
    putStrLn "--------------------------------------"
    hFlush stdout
    entrada <- getLine
    case entrada of
      "1" -> do
        --EXIBE TODOS OS LIVROS COM DETERMINADO TÍTULO OU AUTOR
        putStrLn "Digite o nome do autor ou titulo do livro: "
        hFlush stdout
        entrada2 <- getLine
        putStrLn ("Livros encontrados:  "++ imprimeLivros(buscaLivros livrosDaBiblioteca entrada2))
        menu livrosDaBiblioteca livrosEmprestados
      "2" -> do
        --REALIZA O EMPRÉSTIMO
        putStrLn "Digite o titulo do livro: "
        hFlush stdout
        entrada3 <- getLine
        putStrLn "Digite o nome do autor do livro: "
        hFlush stdout
        entrada4 <- getLine
        let (sucesso,novaBiblioteca,novosEmprestimos) = emprestarLivro (Livro entrada3 entrada4) (livrosDaBiblioteca, livrosEmprestados)
        if (sucesso)
          then do
            print "Emprestimo realizado"
        else putStrLn "Nao foi possivel realizar o emprestimo"
        menu novaBiblioteca novosEmprestimos
      "3" -> do
        --REALIZA A DEVOLUÇÃO
        putStrLn "Digite o titulo do livro: "
        hFlush stdout
        entrada3 <- getLine
        putStrLn "Digite o nome do autor do livro: "
        hFlush stdout
        entrada4 <- getLine
        let (sucesso,novaBiblioteca,novosEmprestimos) = devolverLivro (Livro entrada3 entrada4) (livrosDaBiblioteca, livrosEmprestados)
        if (sucesso)
          then do
            print "Devolucao realizado"
        else putStrLn "Nao foi possivel realizar devolucao"
        menu novaBiblioteca novosEmprestimos
      "4" -> do
        --LISTA OS LIVROS EMPRESTADOS
        putStrLn ("Livros emprestados: " ++ imprimeLivros (livrosEmprestados))
        menu livrosDaBiblioteca livrosEmprestados
      "5" -> do
        --ADICIONA UM NOVO LIVRO NA BIBLIOTECA
        putStrLn "Digite o titulo do livro: "
        hFlush stdout
        entrada3 <- getLine
        putStrLn "Digite o nome do autor do livro: "
        hFlush stdout
        entrada4 <- getLine
        let livro = (Livro entrada3 entrada4)
        let novaBiblioteca = livro:livrosDaBiblioteca
        menu novaBiblioteca livrosEmprestados
      "0" -> do
        --ENCERRA O PROGRAMA
        putStrLn "Saindo..."
      _ -> do
        putStrLn "Opcao invalida...."
        menu livrosDaBiblioteca livrosEmprestados
-------------------------------------------------------------------
main :: IO()
main = menu livrosDaBiblioteca livrosEmprestados