Warcaby

autor: Wojciech Kumoń

Projekt obejmuje grę między komputerem i użytkownikem, a także grę pomiędzy komputerem a komputerem.
Są 3 główne pliki, które należy kompilować:
Checkers.hs          <- plik wyświetlający tylko output wykonanego ruchu (w formacie takim jak w poleceniu)
CheckersWithBoard.hs <- podobnie jak wyżej z tym, że po każdym ruchu wyświetlana jest plansza, żeby widzieć co się dzieje i na tej podstawie decydować
CheckersCpuVsCpu.hs  <- plik umożliwiający zobaczenie rozgrywki komputer kontra komputer

Kompilacja:
ghc -o Checkers Checkers.hs
ghc -o CheckersWithBoard CheckersWithBoard.hs
ghc -o CheckersCpuVsCpu CheckersCpuVsCpu.hs

Pierwsze dwa programy można uruchomić bez argumentów (wtedy gramy białymi). Aby zagrać czarnymi musimy wysłać jeden argument o wartości "b" (od black), w przeciwnym razie będą to białe.
Program, w którym gra komputer z komputerem nie przyjmują argumentów.

Program spełnia zasady gry w warcaby, np. nie pozwala na wykonanie nienajdłuższego bicia. Gra toczona jest aż do zwycięstwa jednego z graczy. AI wykorzystuje algorytm minimax, domyślnie drzewo liczone jest dla 5 ruchów do przodu (kontroluje to zmienna maxTreeDepth w pliku Minimax.hs). Dla każdej planszy obliczany jest wynik punktowy dla białych i czarnych wg algorytmu:
- damka = 25 punktów
- pionek = 5 punktów + indeks (od zera) rzędu jak daleko znajduje się swojego końca planszy co daje zakres 5-12, bo będąc na końcu (indeks 7) natępuje zmiana w damkę czyli na 25 punktów

Wewnętrznie plansza to zbiór pól, wykorzystywa jest lista list (indeksy 0-7), więc dane wejściowe typu MovePDN (10,14) zostają skonwertowane do Move ((2,3), (3,2)).