# Wartość zagrożona z wykorzystaniem pakietu R
Przegląd najbardziej popularnych metod szacowania wartości zagrożonej (Value at Risk), wraz ze 
stworzeniem gotowego narzędzia w postaci skryptu R oraz aplikacji Shiny (opcjonalnie).

## Metody szacowania
* Wariancji-kowariancji (Delta normal)
* Symylacji historycznej
* Monte Carlo

## Dane
Dane użyte w naszym projekcie zawierają kwotowania wszystkich spółek notowanych na Giełdzie Papierów Wartościowych w Warszawie od momentu pierwszej sesji (11 kwiecień 1991 r.). Dane są uzupełniane na bieżąco po zakończeniu kolejnych sesji.

## Interakcja z użytkownikiem
Użytkownik będzie mógł skorzystać ze stworznej przez nas aplikacji Shiny, w której będzie mógł indywidualnie zdefiniować portfel spółek, na podstawie których obliczona ma być wartość zagrożona. Akcje wchodzące w skład portfela mogą być wprowadzone ręcznie, bądź na podstawie dostępnych indeksów giełdowych (np. WIG20, WIG Budownictwo itd.).
