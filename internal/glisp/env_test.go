package glisp

import "testing"

func TestSet(t *testing.T) {
	env := NewEnv(nil)

	expr1 := &Atom{name: "expr1"}
	env.Set("first", expr1)

	expr2 := &Atom{name: "expr2"}
	env.Set("second", expr2)

	expr3 := &Number{value: 123}
	env.Set("third", expr3)

	checkIsPresent(t, env, "env", "first", expr1)
	checkIsPresent(t, env, "env", "second", expr2)
	checkIsPresent(t, env, "env", "third", expr3)

	checkNotPresent(t, env, "env", "one")
	checkNotPresent(t, env, "env", "two")
	checkNotPresent(t, env, "env", "three")
}

func TestIndirectSet(t *testing.T) {
	top := NewEnv(nil)
	mid := NewEnv(top)
	low := NewEnv(mid)

	lowExpr := &Atom{name: "low"}
	low.Set("low", lowExpr)

	midExpr := &Atom{name: "mid"}
	mid.Set("mid", midExpr)

	topExpr := &Atom{name: "top"}
	top.Set("top", topExpr)

	checkIsPresent(t, low, "low", "low", lowExpr)
	checkIsPresent(t, low, "low", "mid", midExpr)
	checkIsPresent(t, low, "low", "top", topExpr)

	checkNotPresent(t, mid, "mid", "low")
	checkIsPresent(t, mid, "mid", "mid", midExpr)
	checkIsPresent(t, mid, "mid", "top", topExpr)

	checkNotPresent(t, top, "top", "low")
	checkNotPresent(t, top, "top", "mid")
	checkIsPresent(t, top, "top", "top", topExpr)
}

func TestSetTopLevel(t *testing.T) {
	top := NewEnv(nil)
	mid := NewEnv(top)
	low := NewEnv(mid)

	lowExpr := &Atom{name: "low"}
	low.SetTop("low", lowExpr)

	midExpr := &Atom{name: "mid"}
	mid.SetTop("mid", midExpr)

	topExpr := &Atom{name: "top"}
	top.SetTop("top", topExpr)

	checkIsPresent(t, low, "low", "low", lowExpr)
	checkIsPresent(t, low, "low", "mid", midExpr)
	checkIsPresent(t, low, "low", "top", topExpr)

	checkIsPresent(t, mid, "mid", "low", lowExpr)
	checkIsPresent(t, mid, "mid", "mid", midExpr)
	checkIsPresent(t, mid, "mid", "top", topExpr)

	checkIsPresent(t, top, "top", "low", lowExpr)
	checkIsPresent(t, top, "top", "mid", midExpr)
	checkIsPresent(t, top, "top", "top", topExpr)
}

func checkIsPresent(t *testing.T, env *Env, envName, key string, expr Expr) {
	v, err := env.Get(&Atom{key})
	if err != nil {
		t.Errorf("Expected to find the key \"%s\" in the \"%s\" env", key, envName)
	}
	if v != expr {
		t.Errorf("Expected to find %v under the key \"%s\" in the \"%s\" env, but got %v", expr, key, envName, v)
	}
}

func checkNotPresent(t *testing.T, env *Env, envName, key string) {
	_, err := env.Get(&Atom{key})
	if err == nil {
		t.Errorf("Retrieved \"%s\" from the \"%s\" env, but it's not supposed to be there", key, envName)
	}
}
