data ShogiPlayer = Sente | Gote deriving (Eq, Show, Enum) 

data ShogiPieza = Rey ShogiPlayer Int Int | Torre Bool ShogiPlayer Int Int | Alfil Bool ShogiPlayer Int Int | Dorado ShogiPlayer Int Int 
				| Lancero Bool ShogiPlayer Int Int | Plateado Bool ShogiPlayer Int Int | Caballo Bool ShogiPlayer Int Int | Peon Bool ShogiPlayer Int Int deriving (Show,Eq)

data Tablero = Tablero [[ShogiPieza]]

data Turno = Turno ShogiPlayer   deriving (Show,Eq)

data Estado = Terminado Int | EnJuego Turno deriving (Show,Eq)

data ShogiAction = Move Bool (Int,Int) (Int,Int) | Drop ShogiPieza (Int,Int) deriving (Show)

data ShogiGame = ShogiGame {tablero :: Tablero , estado :: Estado, GoteWn :: [ShogiPieza], SenteWn :: [ShogiPieza] }

iniciartablero:: Tablero
iniciartablero = Tablero 
			[(Lancero False Sente 9 1),  (Caballo False Sente 9 2 ), (Plateado False Sente 9 3 ),  (Dorado Sente 9 4 ), (Rey Sente 9 5 ), (Dorado Sente 9 4),  (Plateado False Sente 9 3 ), (Caballo False Sente 9 2 ), (Lancero False Sente 9 1),
			 (Torre False Sente 8 2 ), (Alfil False Sente 2 2), (Peon False Sente 7 1),  (Peon False Sente 7 2),  (Peon False Sente 7 3 ),  (Peon False Sente 7 4),  (Peon False Sente 7 5 ),  (Peon False Sente 7 6 ), (Peon False Sente 7 7 ), (Peon False Sente 7 8 ), (Peon False Sente 7 9),
			 (Peon False Gote 3 1), (Peon False Gote 3 2),  (Peon False Gote 3 3), (Peon False Gote 3 4), (Peon False Gote 3 5 ), (Peon False Gote 3 6), (Peon False Gote 3 7), (Peon False Gote 3 8), (Peon False Gote 3 9 ),
			 (Alfil False Gote 2 2 ),  (Torre False Gote 2 8), (Lancero False Gote 1 1), (Caballo False Gote 1 2), (Plateado False Gote 1 3), (Dorado Gote 1 4 ),  (Rey Gote 1 5 ),  (Dorado Gote 1 6 ),  (Plateado False Gote 1 7 ), (Caballo False Gote 1 8), (Lancero False Gote 1 9)]

beginning :: ShogiGame
beginning = ShogiGame (iniciartablero) (EnJuego (Turno Gote)) [] []

activePlayer :: ShogiGame -> Maybe ShogiPlayer
activePlayer  (ShogiGame _ estado)
			| (estado == EnJuego (Turno Sente)) =  Just (Sente)
			| (estado == EnJuego (Turno Gote)) =   Just (Gote)
			| otherwise = Nothing


