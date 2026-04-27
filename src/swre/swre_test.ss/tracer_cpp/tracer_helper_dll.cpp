#include "tracer_helper_dll.h"
#include <Safir/Application/Tracer.h>

static Safir::Application::Tracer bertil(L"Bertil");

class Karin
{
public:
    static Safir::Application::Tracer tracer;
};

Safir::Application::Tracer Karin::tracer(L"Karin");

void tracer_helper_enable(bool e)
{
    bertil.Enable(e);
    Karin::tracer.Enable(e);
}

void tracer_helper_say_something()
{
    bertil << "Hejsan svejsan!" << std::endl;
    Karin::tracer << "Hejsan svejsan!" << std::endl;
}
