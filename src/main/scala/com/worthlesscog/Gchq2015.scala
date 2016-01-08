package com.worthlesscog

import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import org.scalajs.jquery.jQuery
import scala.annotation.tailrec
import scala.scalajs.js.Dynamic.global
import org.scalajs.dom.raw.MouseEvent

object Gchq2015 extends JSApp {

    type Addr = (Int, Int) => Unit
    type Flag = (Int) => Unit
    type Ints = Seq[Int]

    val cols = Seq(
        Seq(7, 2, 1, 1, 7),
        Seq(1, 1, 2, 2, 1, 1),
        Seq(1, 3, 1, 3, 1, 3, 1, 3, 1),
        Seq(1, 3, 1, 1, 5, 1, 3, 1),
        Seq(1, 3, 1, 1, 4, 1, 3, 1),
        Seq(1, 1, 1, 2, 1, 1),
        Seq(7, 1, 1, 1, 1, 1, 7),
        Seq(1, 1, 3),
        Seq(2, 1, 2, 1, 8, 2, 1),
        Seq(2, 2, 1, 2, 1, 1, 1, 2),
        Seq(1, 7, 3, 2, 1),
        Seq(1, 2, 3, 1, 1, 1, 1, 1),
        Seq(4, 1, 1, 2, 6),
        Seq(3, 3, 1, 1, 1, 3, 1),
        Seq(1, 2, 5, 2, 2),
        Seq(2, 2, 1, 1, 1, 1, 1, 2, 1),
        Seq(1, 3, 3, 2, 1, 8, 1),
        Seq(6, 2, 1),
        Seq(7, 1, 4, 1, 1, 3),
        Seq(1, 1, 1, 1, 4),
        Seq(1, 3, 1, 3, 7, 1),
        Seq(1, 3, 1, 1, 1, 2, 1, 1, 4),
        Seq(1, 3, 1, 4, 3, 3),
        Seq(1, 1, 2, 2, 2, 6, 1),
        Seq(7, 1, 3, 2, 1, 1))

    val rows = Seq(
        Seq(7, 1, 1, 3, 7),
        Seq(1, 1, 2, 2, 1, 1),
        Seq(1, 3, 1, 1, 3, 1, 3, 1),
        Seq(1, 3, 1, 6, 1, 1, 3, 1),
        Seq(1, 3, 1, 2, 5, 1, 3, 1),
        Seq(1, 1, 2, 1, 1),
        Seq(7, 1, 1, 1, 1, 1, 7),
        Seq(3, 3),
        Seq(2, 1, 1, 3, 1, 1, 3, 2, 1),
        Seq(1, 1, 2, 3, 1, 1),
        Seq(2, 1, 2, 4, 1, 4),
        Seq(3, 1, 4, 1, 1, 1, 1, 1),
        Seq(5, 2, 1, 1, 1, 2),
        Seq(1, 3, 6, 2, 2, 3),
        Seq(1, 2, 1, 1, 9, 1),
        Seq(1, 3, 2, 2, 1, 2),
        Seq(1, 5, 1, 1, 1, 1, 3),
        Seq(5, 2, 2, 1),
        Seq(3, 1, 1, 1, 2, 1, 7),
        Seq(1, 2, 2, 1, 2, 1, 1),
        Seq(1, 5, 4, 1, 3, 1),
        Seq(2, 10, 3, 1, 3, 1),
        Seq(6, 6, 1, 1, 3, 1),
        Seq(2, 1, 1, 2, 1, 1),
        Seq(5, 2, 1, 2, 7))

    val given = Seq(
        (21, 3), (20, 3), (12, 3), (11, 3), (3, 3), //
        (18, 8), (17, 8), (14, 8), (10, 8), (9, 8), (6, 8), //
        (18, 16), (13, 16), (8, 16), (4, 16), //
        (21, 21), (20, 21), (15, 21), (14, 21), (9, 21), (4, 21), (3, 21))

    val masks = for (
        n <- 1 to math.max(maxRun(rows), maxRun(cols));
        b = (1 << n) - 1;
        l = for (p <- 0 to (cols.size - n)) yield b << p
    ) yield l

    val pow2 = for (n <- 0 to 25) yield (1 << n)

    var c0 = Array.ofDim[Int](cols.size)
    var r0 = Array.ofDim[Int](rows.size)
    var c1 = Array.ofDim[Int](cols.size)
    var r1 = Array.ofDim[Int](rows.size)
    var cs = cols.map(combs(cols.size, _)).reverse.toArray
    var rs = rows.map(combs(rows.size, _)).toArray

    def combs(len: Int, ns: Ints) = {
        def combs(ns: Ints, n: Int, o: Int): Ints = {
            if (ns.isEmpty) List(n)
            else {
                val h = ns.head
                (for (
                    s <- 0 to (len - o - (ns.sum + (ns.size - 1)));
                    pos = o + s;
                    if (pos + h <= len)
                ) yield combs(ns.tail, n + masks(h - 1)(pos), pos + h + 1)).flatten
            }
        }
        combs(ns, 0, 0)
    }

    def bid(c: Int, r: Int) = f"b$c%02d$r%02d"
    def cid(i: Int) = f"c$i%02d"
    def nid(i: Int) = f"n$i%02d"
    def rid(i: Int) = f"r$i%02d"

    def divBut(i: String, f: (MouseEvent) => Unit) = div(cls := "but0", id := i, onclick := f).render
    def colBut(i: Int) = divBut(cid(i), (_) => updateCol(i))
    def rowBut(i: Int) = divBut(rid(i), (_) => updateRow(i))

    def maxRuns(p: Seq[Ints]) = p.map(_.size).max
    def maxRun(p: Seq[Ints]) = p.map(_.max).max

    def WHITE = "c0"
    def BLACK = "c1"
    def UNKNOWN = "c2"

    def buildTable(rs: Seq[Ints], cs: Seq[Ints]) = {
        val nc = cs.size
        val mc = maxRuns(cs)
        val nr = rs.size
        val mr = maxRuns(rs)
        val ce = nr + mc
        val re = mr + nc

        def data(c: Int, r: Int) = {
            val ec = c - mr
            val er = r - mc
            if (r < mc) {
                if (c < mc) td
                else if (c < re) {
                    val ns = cs(ec)
                    td(ns.lift(ns.size - (mc - r - 1) - 1).getOrElse("").toString, cls := "vt")
                } else td
            } else if (r < ce) {
                if (c < mr) td(rs(er).lift(mr - c - 1).getOrElse("").toString)
                else if (c < re) td(id := bid(nc - ec - 1, er), cls := UNKNOWN)
                else if (c == re) td(rowBut(er))
                else td(id := nid(er), cls := "nc")
            } else {
                if (c < mr) td
                else if (c < re) td(colBut(nc - ec - 1))
                else td
            }
        }

        val f = tfoot(tr(td(colspan := mc), td(colspan := nc, "Green buttons show affected rows/columns. Click them!")))
        table(f, for (r <- 0 to ce) yield tr(for (c <- 0 to re + 1) yield data(c, r))).render
    }

    def plot(c: String, x: Int, y: Int) { jQuery("#" + bid(x, y)).attr("class", c) }
    def plotBlock(f: Addr)(c: Int, r: Int) {
        if (jQuery("#" + bid(c, r)).attr("class").getOrElse("") != BLACK) {
            plot(BLACK, c, r)
            c1(c) |= pow2(r)
            r1(r) |= pow2(c)
            f(c, r)
        }
    }
    def wipeBlock(f: Addr)(c: Int, r: Int) {
        if (jQuery("#" + bid(c, r)).attr("class").getOrElse("") != WHITE) {
            plot(WHITE, c, r)
            c0(c) |= pow2(r)
            r0(r) |= pow2(c)
            f(c, r)
        }
    }

    def flagBut(i: String) { jQuery("#" + i).attr("class", "but1") }
    def flagCol(c: Int, r: Int) = flagBut(cid(c))
    def flagRow(c: Int, r: Int) = flagBut(rid(r))
    def flagGiven: Addr = {
        case (c, r) => flagCol(c, r); flagRow(c, r)
    }

    def wipeBut(i: String) { jQuery("#" + i).attr("class", "but0") }
    def wipeCol(i: Int) = wipeBut(cid(i))
    def wipeRow(i: Int) = wipeBut(rid(i))

    def addInts(ns: Ints, x: Int, max: Int, p: Addr, w: Addr) {
        def draw(a: Addr, b: Int, i: Int = 0) {
            if (b > 0) {
                if ((b & 1) > 0) {
                    a(i, x)
                }
                draw(a, b >> 1, i + 1)
            }
        }
        draw(p, ns.reduce(_ & _))
        draw(w, ~ns.reduce(_ | _) & (pow2(max) - 1))
    }

    def and(m: Int)(n: Int) = (n & m) == m
    def not(m: Int, p: Int)(n: Int) = (~n & (pow2(p) - 1) & m) == m

    def updateCol(c: Int) {
        wipeCol(c)
        cs(c) = cs(c) filter and(c1(c)) filter not(c0(c), rows.size)
        addInts(cs(c), c, rows.size, (c, r) => plotBlock(flagRow)(r, c), (c, r) => wipeBlock(flagRow)(r, c))
    }

    def updateRow(r: Int) {
        wipeRow(r)
        rs(r) = rs(r) filter and(r1(r)) filter not(r0(r), cols.size)
        addInts(rs(r), r, cols.size, plotBlock(flagCol), wipeBlock(flagCol))
        jQuery("#" + nid(r)) text rs(r).size.toString
    }

    def setup {
        jQuery("body") append buildTable(rows, cols)
        given foreach { t => plotBlock(flagGiven)(t._1, t._2) }
    }

    def main() {
        jQuery(setup _)
    }

}
