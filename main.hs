module Main where

import System.IO

type NomeAutor = String
type Titulo = String


-- Definição do tipo de dados Livro
data Livro = Livro {
    titulo :: Titulo,
    autor :: NomeAutor,
    emprestado :: Bool
} deriving Show

----------------------------------------------------------------
--livros que tem na biblioteca
livrosDaBiblioteca :: [Livro]
livrosDaBiblioteca = [
    Livro "O sertão vai virar mar" "Moacyr Scliar" True,
    Livro "O sertão vai virar mar" "Moacyr Scliar" True,
    Livro "Água viva" "Clarice Lispector" True,
    Livro "Terror na Festa" "Janaína Amado" True
    ]
----------------------------------------------------------------
--emprestimoLivro :: [Livro]
----------------------------------------------------------------
--função para buscar livros
buscaLivros ::  [Livro] -> String -> [Livro]
buscaLivros [] _ = []
buscaLivros (livro: resto) nome
  | titulo livro == nome = livro : resto
  | autor livro == nome = livro : resto
  | otherwise = buscaLivros resto nome
--where encontrados = snd (buscaLivros resto nome)
----------------------------------------------------------------
--função para buscar um livro específico
buscaLivro ::  [Livro] -> String -> String-> Livro
buscaLivro [] _ _ = Livro " " " " False
buscaLivro (livro: resto) nome escritor =
  if titulo livro == nome &&  autor livro == escritor then livro
  else buscaLivro resto nome escritor
----------------------------------------------------------------
--função para emprestar livro
--emprestimoLivro :: (buscaLivro) -> [Livro]
--emprestimoLivro buscaLivro = buscaLivro !emprestado 
-----------------------------------------------------------------
-- opções do menu
--opcao1 :: IO ()

-----------------------------------------------------------------
--menu do sistema...

menu :: IO ()
menu = do
    putStrLn "-----SEJA BEM VINDO AO SIBLIB-----\n"
    putStrLn "|  O QUE DESEJA FAZER?           |\n"
    putStrLn "|  1 - BUSCA DE LIVRO            |\n"
    putStrLn "|  2 - EMPRESTIMO DE LIVRO       |\n"
    putStrLn "|  3 - DEVOLUCAO DE LIVRO        |\n"
    putStrLn "|  0 - SAIR                      |\n"
    putStrLn "----------------------------------"
    hFlush stdout
    entrada <- getLine
    case entrada of
      "1" -> do
        putStrLn "Digite o nome do autor ou título do livro: "
        hFlush stdout
        entrada2 <- getLine
        putStrLn ("Resultado para "++show entrada2 ++" "++show(buscaLivros livrosDaBiblioteca entrada2))
        menu
      "2" -> do
        putStrLn "Você escolheu a opção 2"
        menu
      "3" -> do
        putStrLn "Você escolheu a opção 3"
        menu
      "0" -> do
        putStrLn "Saindo..."
      _ -> do
        putStrLn "Opção inválida...."
        menu
-------------------------------------------------------------------
main :: IO()
main = do
  menu
