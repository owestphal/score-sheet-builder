/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./dev/index.html",
    "./output/Main/*.js",
    "./output/App.{MainUI,PlayerSheet,ConfigEditor}/*.js"
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}

