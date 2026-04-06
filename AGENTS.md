# Структура проекта

* `src/` - haskell код
* `static/` - всякая статика, html, css
* `static/index.css` - основной файл со стилями приложения
* `public/` - место куда собираются 
* `DESIGN.md` - дизайн

# Тех стек
Проект делается на Haskell (GHC 9.12) + miso 1.9.0
Собирается в web assembly с помощью `nix`

# Работа с кодом
При изменении верстки отражай изменения в наброске дизайна в DESIGN.md

# Как собирать

## Публичный билд
```bash
nix develop .#wasm --command bash -c 'make build'
```

## Отладка визуала через Playwright

1. Собрать: `nix develop .#wasm --command bash -c 'make build'`
2. Запустить файловый сервер: `python3 -m http.server 9933 --directory public &`
3. Открыть в playwright: `playwright-cli open http://localhost:9933/index.html`
4. Подождать загрузки wasm (~5 сек): `sleep 5 && playwright-cli snapshot`
5. Для проверки размеров элементов: `playwright-cli eval "() => JSON.stringify([...document.querySelectorAll('.class')].map(e => ({w: e.offsetWidth})))"`
6. По окончании: `playwright-cli close` и убить сервер `kill %1`

**Ограничение:** нельзя открывать `file://` — только через HTTP-сервер.
