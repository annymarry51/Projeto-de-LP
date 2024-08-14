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
    Livro "Água viva" "Clarice Lispector" True,
    Livro "Terror na Festa" "Janaína Amado" True
    ]
----------------------------------------------------------------
--função para buscar livros
buscaLivros ::  [Livro] -> String -> [Livro]
buscaLivros [] _ = []
buscaLivros (livro: resto) nome
  | titulo livro == nome = livro : buscaLivros resto nome
  | autor livro == nome = livro : buscaLivros resto nome
  | otherwise = buscaLivros resto nome
----------------------------------------------------------------
--função para buscar um livro específico
buscaLivro ::  [Livro] -> String -> String-> Livro
buscaLivro [] _ _ = Livro " " " " False
buscaLivro (livro: resto) nome escritor =
  if titulo livro == nome &&  autor livro == escritor then livro
  else buscaLivro resto nome escritor
----------------------------------------------------------------

main :: IO()
main = do
   print ()