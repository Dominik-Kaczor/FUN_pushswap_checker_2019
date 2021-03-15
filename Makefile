##
## EPITECH PROJECT, 2020
## Doop
## File description:
## Makefile for the doop
##

SRC		=	pushswap_checker.hs

NAME		=	pushswap_checker

CC		=	ghc

RM		=	rm -f

all:	$(NAME)

$(NAME):
	$(CC) -o $(NAME) $(SRC)

clean:
	$(RM) pushswap_checker.hi
	$(RM) pushswap_checker.o

fclean: clean
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
