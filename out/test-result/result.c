/* 
  This file was generated by KaRaMeL <https://github.com/FStarLang/karamel>

  F* version: <unknown>

 */

#include "result.h"

result_S result_ident(result_S x)
{
  return x;
}

result_S result_mk1(uint32_t x, uint32_t y)
{
  return
    (KRML_CLITERAL(result_S){ .tag = result_S1, .val = { .case_S1 = { .x1 = x, .y1 = y } } });
}

void result_main(void)
{
  result_S uu____0 = result_mk1(0U, 0U);
  if (uu____0.tag == result_S1)
  {
    switch (uu____0.val.case_S1.x1)
    {
      case 0U:
        {
          switch (uu____0.val.case_S1.y1)
          {
            case 0U:
              {
                result_S
                uu____1 =
                  result_ident((
                      KRML_CLITERAL(result_S){
                        .tag = result_S2,
                        .val = { .case_S2 = { .x2 = 0U, .y2 = 0U } }
                      }
                    ));
                if (uu____1.tag == result_S2)
                {
                  switch (uu____1.val.case_S2.x2)
                  {
                    case 0U:
                      {
                        switch (uu____1.val.case_S2.y2)
                        {
                          case 0U:
                            {
                              return;
                            }
                          default:
                            {

                            }
                        }
                        break;
                      }
                    default:
                      {

                      }
                  }
                }
                KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n", __FILE__, __LINE__, "panic!");
                KRML_HOST_EXIT(255U);
                break;
              }
            default:
              {

              }
          }
          break;
        }
      default:
        {

        }
    }
  }
  KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n", __FILE__, __LINE__, "panic!");
  KRML_HOST_EXIT(255U);
}

