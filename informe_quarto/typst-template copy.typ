#let report(
    title: none,
    date: none,
    content
)= {
    set page(
        paper: "us-letter",
        margin: (top: 1in, bottom: 1in)
    )

    set text(
        lang: "es",
        region: "es",
        font: "Times New Roman",
        size: 12pt,
    )

    show heading: it => {
        let sizes = (
            "1": 16pt,
            "2": 10pt
        )

        let level = str(it.level)
        let size = sizes.at(level)
        let formatted_heading = if level == "2" {it} else {upper(it)}
        let alignment = if level == "2" {center} else {left}

        set text(
            font: "Times New Roman",
            fill: rgb("002D72"),
            size: size,
            weight: "bold"
        )

        align(alignment)[#formatted_heading]

    }
    content
}