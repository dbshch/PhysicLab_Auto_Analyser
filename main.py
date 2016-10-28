#!/usr/bin/python
# __author__ = 'dbshch'

import os

import tornado.ioloop
import tornado.web


class mainHandler(tornado.web.RequestHandler):
    def get(self):
        self.render("index.html")


class analyser(tornado.web.RequestHandler):
    def post(self):
        args = self.get_argument('args').split(';')
        code = "Rscript lab5.R"
        for i in range(17):
            code = code + ' ' + args[i]
        os.system(code)
        fp = open("foo.txt", "r")
        Tb1 = fp.readline()[:-1].split(';')
        fp.readline()
        Tb2 = fp.readline()[:-1].split(';')
        fp.readline()
        Tb3 = fp.readline()[:-1].split(';')
        fp.readline()
        Tb4 = fp.readline()[:-1].split(';')
        fp.close()
        fp = open("table.txt", "r")
        table = fp.readlines()
        fp.close()
        os.listdir("./static")
        self.render("result.html",
                    R='%.2f' % float(args[0]),
                    f1='%.3f' % float(args[1]),
                    E1='%.2f' % float(args[2]),
                    C='%.2f' % float(args[3]),
                    T1='%.3f' % float(args[4]),
                    Tb1=Tb1,
                    Tb2=Tb2,
                    Tb3=Tb3,
                    f2='%.3f' % float(args[5]),
                    E2='%.2f' % float(args[6]),
                    T2='%.2f' % float(args[7]),
                    E3='%.2f' % float(args[8]),
                    f3='%.3f' % float(args[9]),
                    T3='%.2f' % float(args[10]),
                    E4='%.2f' % float(args[11]),
                    table=table,
                    Tb4=Tb4
                    )


def make_app():
    settings = {"static_path": os.path.join(os.path.dirname(__file__), "static")}
    return tornado.web.Application([
        (r"/", mainHandler),
        (r"/analyser", analyser)
    ], **settings)


if __name__ == "__main__":
    app = make_app()
    app.listen(13000)
    tornado.ioloop.IOLoop.current().start()
