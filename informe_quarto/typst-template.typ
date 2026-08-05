#let report(
  title: none,
  date: none,
  author: none,
  content,
) = {
  // ------------------------------------------------------------
  // Configuración general
  // ------------------------------------------------------------

  set text(
    lang: "es",
    region: "es",
    font: "Times New Roman",
    size: 12pt,
    fill: black,
  )

  // ------------------------------------------------------------
  // Carátula
  // ------------------------------------------------------------

//   set page(
//     paper: "us-letter",
//     margin: 1in,
//     numbering: none,
//     background: place(
//         top,
//         rect(
//             width: 2cm,
//             height: 100%,
//             fill: rgb("00457F"),
//         )
//     )
//   )

//   align(center)[
//     #v(1fr)

//     #if title != none {
//       text(
//         size: 24pt,
//         weight: "bold",
//       )[
//         #title
//       ]
//     }

//     #v(2em)

//     #if author != none {
//       author
//     }

//     #v(1fr)

//     #if date != none {
//       date
//     }

//     #v(1fr)
//   ]

  // ------------------------------------------------------------
  // Comienzo del cuerpo
  // ------------------------------------------------------------

  pagebreak()

  set page(
    paper: "us-letter",
    margin: 1in,
    numbering: "1",
    number-align: top + right,
    background: none
  )

  counter(page).update(1)

  set par(
    justify: false,
    leading: 1em,
    spacing: 6pt,
    first-line-indent: (
      amount: 0.5in,
      all: true,
    ),
  )

  set heading(
    numbering: "1.1.1",
  )

  show heading.where(level: 1): set align(center)
  show heading.where(level: 1): set text(
    size: 12pt,
    weight: "bold",
  )

  show heading.where(level: 2): set align(left)
  show heading.where(level: 2): set text(
    size: 12pt,
    weight: "bold",
  )

  show heading.where(level: 3): set align(left)
  show heading.where(level: 3): set text(
    size: 12pt,
    weight: "bold",
    style: "italic",
  )

  content
}
